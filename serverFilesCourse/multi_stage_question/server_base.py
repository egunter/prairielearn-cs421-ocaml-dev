import json
import chevron
from shared_utils import QuestionData
from typing import TypedDict, Optional, Any
import shared_utils as su
from abc import ABC, abstractmethod
# handles the logic for generating the stages as well as grading the stages.


# create a class called StageDict that inherits from TypedDict
class StageDict(TypedDict):
    is_upcoming: bool
    is_current: bool
    score: float
    is_finished: bool
    attempts: int


class StageHandler(ABC):
    @abstractmethod
    def generate_initial_stage(self) -> tuple[list[StageDict], int, int, int]:
        pass

    @abstractmethod
    def generate_next_stage(self, stages: list[StageDict], current_stage_num: int, num_stages: int) -> StageDict:
        pass

    @abstractmethod
    def grade_current_stage(self, stage_info: StageDict, question_name: str, data: QuestionData, current_stage_num: int) -> float:
        pass

    @abstractmethod
    def updateDisplay(self, stages: list[StageDict], current_stage_num: int, data: QuestionData) -> None:
        pass

    @abstractmethod
    def toJSON(self) -> dict[str, Any]:
        pass

    @abstractmethod
    def fromJSON(cls, data: dict[str, Any]):
        pass


# create a class called MultiStageQuestion that contains a stage_handler, and a list of Typed dicts called stage info
class MultiStageQuestion:
    stages: list[StageDict]
    current_stage_num: int
    num_stages: int
    attempts_allowed: int

    def __init__(self, stage_handler: Optional[StageHandler] = None, json_string: Optional[str] = None):
        if json_string is not None:
            self.fromJSON(json_string)
            return
        if stage_handler is not None:
            self.stages, self.current_stage_num, self.num_stages, self.attempts_allowed = stage_handler.generate_initial_stage()
            return
        raise ValueError("Must provide either a stage handler or a json string")

    def set_stages_current(self) -> None:
        "Set current entry in row list by idx"
        cur_stage = self.stages[self.current_stage_num]
        cur_stage['is_current'] = True
        cur_stage['is_upcoming'] = False

    def get_question_name(self, question_name: str, data: su.QuestionData) -> str:
        return f"{self.current_stage_num}-{question_name}"

    def set_stage_finished(self, stage) -> None:
        "Change row_dict to finished status. is_upcoming should already be false."
        stage['is_finished'] = True
        stage['is_current'] = False

    def generate(self, data: QuestionData) -> None:
        print("inside generate")

    def grade(self, data: QuestionData, stage_handler: StageHandler) -> None:

        data['params']['last_submission_correct'] = True

        # Only do something if we haven't done the whole table yet
        if self.current_stage_num < self.num_stages:
            # Grade current row
            question_name = self.get_question_name("answer", data)

            current_stage = self.stages[self.current_stage_num]

            # Grade the current stage
            current_stage['score'] = stage_handler.grade_current_stage(current_stage, question_name, data, self.current_stage_num)

            # Only increment attempts if the student's input is valid
            if not data['params'].get('invalid_format', False):
                current_stage["attempts"] += 1

            data['score'] = current_stage['score']

            if data['params'].get('invalid_format', False):
                # Invalid format; do not deduct attempt or proceed
                data["params"]["last_submission_correct"] = False
                # Keep the current stage as is
            elif data['score'] == 1:
                data["params"]["feedback"] = "Correct. Move on to the next operation!"
                # Set current row to be finished
                self.set_stage_finished(current_stage)
                self.current_stage_num += 1
                # Generate next stage if we still have stages to go
                if self.current_stage_num < self.num_stages:
                    self.stages.append(stage_handler.generate_next_stage(self.stages, self.current_stage_num, self.num_stages))
                    self.set_stages_current()
                else:
                    data["params"]["is_done"] = True
            elif data['score'] == 0 and current_stage['attempts'] >= self.attempts_allowed:
                data["params"]["feedback"] = "You have used all of your attempts. The correct answer has been given. Please try the next operation."
                # Set current row to be finished
                self.set_stage_finished(current_stage)
                self.current_stage_num += 1
                # Generate next stage if we still have stages to go
                if self.current_stage_num < self.num_stages:
                    self.stages.append(stage_handler.generate_next_stage(self.stages, self.current_stage_num, self.num_stages))
                    self.set_stages_current()
                else:
                    data["params"]["is_done"] = True
            else:
                data["params"]["feedback"] = "Incorrect. Please try again."
                data["params"]["last_submission_correct"] = False

        # Recalculate the overall score
        data['score'] = sum(
            stage["score"]
            for stage in self.stages[:self.current_stage_num]
        ) / self.num_stages

        stage_handler.updateDisplay(self.stages, self.current_stage_num, data)

    def get_mustache_path(self, data: QuestionData) -> str:
        return data["params"]["mustache_path"]

    def __getstate__(self) -> dict[str, Any]:
        # return a dictionary of the object's attributes without using self.__dict__
        return {
            "stages": self.stages,
            "current_stage_num": self.current_stage_num,
            "num_stages": self.num_stages,
            "attempts_allowed": self.attempts_allowed
        }

    def toJSON(self) -> str:
        return json.dumps(self.__getstate__())

    def fromJSON(self, json_string: str):
        data = json.loads(json_string)
        self.stages = data["stages"]
        self.current_stage_num = data["current_stage_num"]
        self.num_stages = data["num_stages"]
        self.attempts_allowed = data["attempts_allowed"]


def generate(data: QuestionData, stage_handler: StageHandler) -> None:
    if "multi_stage_question" not in data["params"]:
        multi_stage_question = MultiStageQuestion(stage_handler)
    else:
        multi_stage_question = MultiStageQuestion(json_string=data["params"]["multi_stage_question"])

    data["params"]["multi_stage_question"] = multi_stage_question.toJSON()
    stage_handler.updateDisplay(multi_stage_question.stages, multi_stage_question.current_stage_num, data)
    
    with open(multi_stage_question.get_mustache_path(data)) as f:
        data["params"]["html"] = chevron.render(f, data['params']).strip()


def grade(data: su.QuestionData, stage_handler: StageHandler) -> None:
    # deserialize the multi_stage_question into a MultiStageQuestion object
    multi_stage_question = MultiStageQuestion(json_string=data["params"]["multi_stage_question"])
    multi_stage_question.grade(data, stage_handler)
    # serialize the MultiStageQuestion object back into a string
    data["params"]["multi_stage_question"] = multi_stage_question.toJSON()
    stage_handler.updateDisplay(multi_stage_question.stages, multi_stage_question.current_stage_num, data)
    with open(multi_stage_question.get_mustache_path(data)) as f:
        data["params"]["html"] = chevron.render(f, data['params']).strip()
