from multi_stage_question.server_base import StageHandler, StageDict
import multi_stage_question.server_base as server_base
from shared_utils import QuestionData
from typing import Optional, TypedDict, Any


class QuesStageInfo(TypedDict):
    question: Any
    answer: Any
    is_correct: bool
    student_answer: str
    attempts_left: Optional[int]


# create a class called heap_handler that inherits from StageHandler
class SimpleStageHandler(StageHandler):

    def __init__(self, grader_function, questions: list[str] = None, answers: list[str] = None):
        self.grader_function = grader_function
        if questions is not None and answers is not None:
            self.questions = questions
            self.answers = answers
            self.num_stages = len(questions)
            self.attempts_allowed: int = 3

            self.ques_stages: list[QuesStageInfo] = []
            self.current_state: str = ""

    def generate_initial_stage(self):

        stage_info = {
            'is_upcoming': False,
            'is_current': True,
            'score': 0,
            'is_finished': False,
            'attempts': 0
        }
        ques_stage_info: QuesStageInfo = QuesStageInfo({
            'question': self.questions[0],
            'answer': self.answers[0],
            'is_correct': False,
            'student_answer': "",
            'attempts_left': None
        })
        self.ques_stages = [ques_stage_info]

        return [stage_info], 0, self.num_stages, self.attempts_allowed

    def generate_next_stage(self, stages: list[StageDict], current_stage_num: int, num_stages: int) -> StageDict:
        # convert the stages and ques_data to the correct types

        next_stage_info = {
            'score': 0,
            'is_upcoming': False,
            'is_current': False,
            'is_finished': False,
            'attempts': 0
        }

        ques_stage_info: QuesStageInfo = QuesStageInfo({
            'question': self.questions[current_stage_num],
            'answer': self.answers[current_stage_num],
            'is_correct': False,
            'student_answer': "",
            'attempts_left': None
        })
        self.ques_stages.append(ques_stage_info)

        return next_stage_info


    def grade_current_stage(self, stage_info: StageDict, question_name: str, data: QuestionData, current_stage_num: int) -> float:
        # convert the stage_info to the correct type
        ques_stage_info: QuesStageInfo = self.ques_stages[current_stage_num]

        student_answer = data['submitted_answers'][question_name]

        # compare the user's answer to the correct answer(stage_info["heap_state"])
        # convert the user's answer to a list of numbers
        # convert student answer of form "1,2,3" to list of ints
        if self.grader_function is not None:
            score = self.grader_function(ques_stage_info["answer"], student_answer)
            if score == 1:
                ques_stage_info["is_correct"] = True
            else:
                ques_stage_info["student_answer"] = student_answer
            return score

        if student_answer == ques_stage_info["answer"]:
            ques_stage_info["is_correct"] = True
            return 1
        else:
            ques_stage_info["student_answer"] = student_answer
            return 0

    # update any display variables that should be updated when moving to the next stage.
    def updateDisplay(self, stages: list[StageDict], current_stage_num: int, data: QuestionData) -> None:
        # if current stage num is not zero, set the current state to the ques state of the previous stage

        if current_stage_num != 0:
            # print heap state for every stage in heap_stages
            self.current_state = self.ques_stages[current_stage_num - 1]["answer"]

        # add "attempts_left" tp the stage info if current stage is in bounds
        if current_stage_num < len(stages):
            self.ques_stages[current_stage_num]["attempts_left"] = 3 - stages[current_stage_num]["attempts"]

        # combine stages
        stages_display: list[dict[str, Any]] = [{
            'question': self.ques_stages[i]['question'],
            'answer': self.ques_stages[i]['answer'],
            'is_correct': self.ques_stages[i]['is_correct'],
            'student_answer': self.ques_stages[i]['student_answer'],
            'attempts_left': self.attempts_allowed - stages[i]['attempts'],
            'attempts': stages[i]['attempts'],
            'is_finished': stages[i]['is_finished'],
            'is_current': stages[i]['is_current'],
        } for i in range(len(stages))]

        data["params"]["stages"] = stages_display
        data["params"]["current_stage_num"] = current_stage_num
        data["params"]["current_state_display"] = self.current_state

    def toJSON(self) -> dict[str, Any]:
        """
        Serialize the HeapHandler object to a JSON-formatted string.
        """
        data = {
            'num_stages': self.num_stages,
            'questions': self.questions,
            'answers': self.answers,
            'ques_stages': self.ques_stages,
            'attempts_allowed': self.attempts_allowed,
            'current_state': self.current_state
        }
        return data

    @classmethod
    def fromJSON(cls, data: dict[str, Any]):
        """
        Deserialize a JSON-formatted string to a HeapHandler object.
        """

        ques_handler = cls(grader_function=None)
        ques_handler.num_stages = data['num_stages']
        ques_handler.questions = data['questions']
        ques_handler.answers = data['answers']
        ques_handler.ques_stages = data['ques_stages']
        ques_handler.attempts_allowed = data['attempts_allowed']
        ques_handler.current_state = data['current_state']
        return ques_handler


def generate(data: QuestionData, questions, answers, grader_function) -> None:
    data["params"]["mustache_path"] = data["options"]["question_path"] + "/question_base.mustache"
    # create instance of heap_handler
    if "stage_handler" not in data["params"]:
        ques_handler = SimpleStageHandler(grader_function, questions, answers)
        data["params"]["stage_handler"] = ques_handler.toJSON()

    server_base.generate(data, ques_handler)
    data["params"]["stage_handler"] = ques_handler.toJSON()


def grade(data: QuestionData, grader_function) -> None:
    # from json
    ques_handler = SimpleStageHandler.fromJSON(data["params"]["stage_handler"])
    ques_handler.grader_function = grader_function
    server_base.grade(data, ques_handler)

    # update the stage handler
    data["params"]["stage_handler"] = ques_handler.toJSON()
