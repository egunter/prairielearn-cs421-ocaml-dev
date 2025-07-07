from multi_stage_question.server_base import StageHandler, StageDict
import multi_stage_question.server_base as server_base
from shared_utils import QuestionData
from typing import Tuple, Optional, TypedDict, Any
import re


class PicomlStageInfo(TypedDict):
    code: str
    correct_env: str
    student_answer: str
    is_correct: bool
    attempts_left: int


class PicomlEnvHandler(StageHandler):

    def __init__(self):
        self.stages = [
            {
                'code': 'let x = 4;;',
                'correct_env': '{x -> 4}'
            },
            {
                'code': '''let y = 
let x = 16 in 
    x - 5;;''',
                'correct_env': '{x -> 4, y -> 11}'
            },
            {
                'code': 'let f x = y + x;;',
                'correct_env': '{x -> 4, y -> 11, f -> <x -> y + x, {x -> 4, y -> 11}>}'
            },
            {
                'code': 'f y;;',
                'correct_env': '{x -> 4, y -> 11, f -> <x -> y + x, {x -> 4, y -> 11}>}'
            },
            {
                'code': 'let y = f (2 * x);;',
                'correct_env': '{ x -> 4, y -> 19, f -> <x -> y + x, {y -> 11, x -> 4}>}'
            },
            {
                'code': 'let twice g x = g (g x);;',
                'correct_env': '{f -> <x -> y  +  x, {x -> 4, y -> 11}>, twice -> <g -> fun x-> g (g x), {f -> <x -> y  +  x, {x -> 4, y -> 11}>, x -> 4, y -> 19}>, x -> 4, y -> 19}'
            },
            {
                'code': 'let f = f x;;',
                'correct_env': '{ f -> 15, twice -> <g -> fun x-> g (g x), {f -> <x -> y  +  x, {x -> 4, y -> 11}>, x -> 4, y -> 19}>, x -> 4, y -> 19}'
            }
        ]
        self.num_stages = len(self.stages)
        self.stages_info: list[PicomlStageInfo] = []
        self.attempts_allowed = 3

    def generate_initial_stage(self):
        stage_info = {
            'is_upcoming': False,
            'is_current': True,
            'score': 0,
            'is_finished': False,
            'attempts': 0
        }
        stage_data = self.stages[0]
        picoml_stage_info = PicomlStageInfo({
            'code': stage_data['code'],
            'correct_env': stage_data['correct_env'],
            'student_answer': '',
            'is_correct': False,
            'attempts_left': self.attempts_allowed
        })
        self.stages_info = [picoml_stage_info]
        return [stage_info], 0, self.num_stages, self.attempts_allowed

    def generate_next_stage(self, stages: list[StageDict], current_stage_num: int, num_stages: int):
        stage_data = self.stages[current_stage_num]
        next_stage_info = {
            'score': 0,
            'is_upcoming': False,
            'is_current': False,
            'is_finished': False,
            'attempts': 0
        }
        picoml_stage_info = PicomlStageInfo({
            'code': stage_data['code'],
            'correct_env': stage_data['correct_env'],
            'student_answer': '',
            'is_correct': False,
            'attempts_left': self.attempts_allowed
        })
        self.stages_info.append(picoml_stage_info)
        return next_stage_info

    def parse_environment(self, env_str: str) -> Optional[dict]:
        """
        Parses an environment string into a dictionary where each key maps
        to a list of values.
        """
        env_str = env_str.strip()
        if not env_str.startswith('{') or not env_str.endswith('}'):
            return None
        env_str = env_str[1:-1].strip()

        pattern = r'''
                \s*(\w+)                    
                \s*->\s*                    
                (
                    <
                        (?:
                            [^<>]*
                            |
                            <[^<>]*>
                        )*
                    >
                    |
                    \{
                        (?:
                            [^{}]*
                            |
                            \{[^{}]*\}
                        )*
                    \}
                    |
                    [^,{}<>]+
                )
            '''

        try:
            tokens = re.findall(pattern, env_str, re.VERBOSE)
            if not tokens:
                return None
        except re.error:
            return None

        bindings = {}
        for key, value in tokens:
            key = key.strip()
            value = self.normalize_closure(value.strip())
            if key not in bindings:
                bindings[key] = []
            bindings[key].append(value)
        return bindings

    def normalize_closure(self, closure: str) -> str:
        if closure.startswith('<') and closure.endswith('>'):
            closure_content = closure[1:-1].strip()
            parts = self.split_closure(closure_content)
            if parts:
                func_part, env_part = parts
                func_part = re.sub(r'\s*->\s*', ' -> ', func_part.strip())
                func_part = re.sub(r'\s+', ' ', func_part.strip())
                env_part = self.normalize_environment(env_part.strip())
                return f"<{func_part}, {env_part}>"
        return closure

    def normalize_environment(self, env_str: str) -> str:
        env_dict = self.parse_environment(env_str)
        if env_dict is None:
            return env_str
        normalized_bindings = []
        for key in sorted(env_dict.keys()):
            vals = env_dict[key]
            normalized_vals = [self.normalize_closure(v) for v in vals]
            for val in normalized_vals:
                normalized_bindings.append(f"{key} -> {val}")
        return f"{{ {', '.join(normalized_bindings)} }}"

    def flatten_environment(self, env: dict) -> list:
        """
        Recursively flatten the environment into a list of (key, value) pairs.
        If a value is an environment or closure, flatten its environment part as well.
        """
        result = []
        for key, values in env.items():
            for val in values:
                # If val is a closure <func_part, { ... }>, parse out the env and flatten it too
                closure_match = re.match(r'^<(.*),(.*)>$', val.strip())
                if closure_match:
                    # It's a closure
                    func_part = closure_match.group(1).strip()
                    closure_env = closure_match.group(2).strip()
                    result.append((key, func_part))
                    # Parse the env part of the closure and flatten
                    closure_env_dict = self.parse_environment(closure_env)
                    if closure_env_dict is not None:
                        result.extend(self.flatten_environment(closure_env_dict))
                elif val.startswith('{') and val.endswith('}'):
                    # It's a nested environment
                    nested_env = self.parse_environment(val)
                    if nested_env is not None:
                        # Insert the key as a marker, if desired, or skip
                        # Flatten the nested environment
                        result.extend(self.flatten_environment(nested_env))
                else:
                    # Simple value
                    result.append((key, val.strip()))
        return result

    def environments_equal(self, env1: dict, env2: dict) -> bool:
        if env1 is None or env2 is None:
            return False
        flat1 = self.flatten_environment(env1)
        flat2 = self.flatten_environment(env2)

        if len(flat1) != len(flat2):
            return False

        # Attempt all permutations of flat2 to match flat1
        # Sort keys in both flats to reduce permutations if possible
        # Convert lists to sorted tuples and compare
        flat1_sorted = sorted(flat1)
        flat2_sorted = sorted(flat2)
        return flat1_sorted == flat2_sorted

    def values_equal(self, val1: str, val2: str) -> bool:
        val1 = self.normalize_closure(val1.strip())
        val2 = self.normalize_closure(val2.strip())
        if val1 == val2:
            return True
        if val1.startswith('<') and val1.endswith('>') and val2.startswith('<') and val2.endswith('>'):
            return self.closures_equal(val1, val2)
        if val1.startswith('{') and val1.endswith('}') and val2.startswith('{') and val2.endswith('}'):
            env1 = self.parse_environment(val1)
            env2 = self.parse_environment(val2)
            return self.environments_equal(env1, env2)
        return False

    def closures_equal(self, closure1: str, closure2: str) -> bool:
        closure1 = closure1[1:-1].strip()
        closure2 = closure2[1:-1].strip()
        parts1 = self.split_closure(closure1)
        parts2 = self.split_closure(closure2)
        if parts1 is None or parts2 is None:
            return False
        func1, env_str1 = parts1
        func2, env_str2 = parts2
        func1 = re.sub(r'\s*->\s*', ' -> ', func1.strip())
        func2 = re.sub(r'\s*->\s*', ' -> ', func2.strip())
        if func1 != func2:
            return False
        env_dict1 = self.parse_environment(env_str1)
        env_dict2 = self.parse_environment(env_str2)
        return self.environments_equal(env_dict1, env_dict2)

    def split_closure(self, closure_str: str) -> Optional[Tuple[str, str]]:
        index = self.find_main_comma(closure_str)
        if index == -1:
            return None
        func_part = closure_str[:index].strip()
        env_part = closure_str[index + 1:].strip()
        return func_part, env_part

    def find_main_comma(self, s: str) -> int:
        bracket_level = 0
        brace_level = 0
        angle_bracket_level = 0
        for i, c in enumerate(s):
            if c == '<':
                angle_bracket_level += 1
            elif c == '>':
                angle_bracket_level -= 1
            elif c == '{':
                brace_level += 1
            elif c == '}':
                brace_level -= 1
            elif c == '(':
                bracket_level += 1
            elif c == ')':
                bracket_level -= 1
            elif c == ',' and bracket_level == 0 and brace_level == 0 and angle_bracket_level == 0:
                return i
        return -1

    def normalize_whitespace(self, code_str: str) -> str:
        code_str = re.sub(r'(fun)([A-Za-z_])', r'\1 \2', code_str)
        return code_str

    def grade_current_stage(self, stage_info: StageDict, question_name: str, data: QuestionData, current_stage_num: int) -> float:
        picoml_stage_info = self.stages_info[current_stage_num]
        original_answer = data['submitted_answers'][question_name]

        student_answer = re.sub(r'\s+', '', original_answer)
        correct_env = re.sub(r'\s+', '', picoml_stage_info['correct_env'])
        student_answer = self.normalize_whitespace(student_answer)
        correct_env = self.normalize_whitespace(correct_env)

        student_env = self.parse_environment(student_answer)
        correct_env_parsed = self.parse_environment(correct_env)

        if student_env is None:
            data['params']['invalid_format'] = True
            data["params"]["feedback"] = "Invalid format. Please enter the environment using the specified syntax."
            return 0.0

        data['params']['invalid_format'] = False
        picoml_stage_info['student_answer'] = original_answer

        if self.environments_equal(student_env, correct_env_parsed):
            picoml_stage_info['is_correct'] = True
            return 1.0
        else:
            return 0.0

    def updateDisplay(self, stages: list[StageDict], current_stage_num: int, data: QuestionData) -> None:
        for i in range(len(stages)):
            self.stages_info[i]['attempts_left'] = self.attempts_allowed - stages[i]['attempts']
        stages_display = [{
            'code': self.stages_info[i]['code'],
            'correct_env': self.stages_info[i]['correct_env'],
            'student_answer': self.stages_info[i]['student_answer'],
            'is_correct': self.stages_info[i]['is_correct'],
            'attempts_left': self.stages_info[i]['attempts_left'],
            'attempts': stages[i]['attempts'],
            'is_finished': stages[i]['is_finished'],
            'is_current': stages[i]['is_current'],
        } for i in range(len(stages))]
        data['params']['stages'] = stages_display
        data['params']['current_stage_num'] = current_stage_num

    def toJSON(self) -> dict[str, Any]:
        data = {
            'stages': self.stages,
            'stages_info': self.stages_info,
            'num_stages': self.num_stages,
            'attempts_allowed': self.attempts_allowed
        }
        return data

    @classmethod
    def fromJSON(cls, data: dict[str, Any]):
        handler = cls()
        handler.stages = data['stages']
        handler.stages_info = data['stages_info']
        handler.num_stages = data['num_stages']
        handler.attempts_allowed = data['attempts_allowed']
        return handler


def generate(data: QuestionData) -> None:
    data["params"]["mustache_path"] = data["options"]["question_path"] + "/question_base.mustache"
    if "stage_handler" not in data["params"]:
        handler = PicomlEnvHandler()
    else:
        handler = PicomlEnvHandler.fromJSON(data["params"]["stage_handler"])

    server_base.generate(data, handler)
    data["params"]["stage_handler"] = handler.toJSON()


def grade(data: QuestionData) -> None:
    handler = PicomlEnvHandler.fromJSON(data["params"]["stage_handler"])
    server_base.grade(data, handler)
    data["params"]["stage_handler"] = handler.toJSON()
