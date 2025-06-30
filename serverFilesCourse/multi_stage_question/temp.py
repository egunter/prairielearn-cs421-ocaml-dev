import json
import chevron
from shared_utils import QuestionData
from typing import TypedDict, Optional, Any
import shared_utils as su
from abc import ABC, abstractmethod

class StageDict(TypedDict):
    is_upcoming: bool
    is_current: bool
    score: float
    is_finished: bool
    attempts: int

class StageHandler(ABC):
    @abstractmethod
    def generate_initial_stage(self):
        pass

    @abstractmethod
    def generate_next_stage(self, stages: list[StageDict], current_stage_num: int, num_stages: int, attempts_allowed: int):
        pass

    @abstractmethod
    def grade_current_stage(self, stage_info: StageDict, question_name: str, data: QuestionData, current_stage_num: int, num_stages: int, attempts_allowed: int) -> float:
        pass

    @abstractmethod
    def updateDisplay(self, stages: list[StageDict], current_stage_num: int, num_stages: int, data: QuestionData) -> None:
        pass

    @abstractmethod
    def toJSON(self) -> dict[str, Any]:
        pass

    @abstractmethod
    def fromJSON(cls, data: dict[str, Any]):
        pass

class MultiStageQuestion:
    def __init__(self, stage_handler: Optional[StageHandler] = None, json_string: Optional[str] = None):
        if json_string is not None:
            data = json.loads(json_string)
            self.stages = data["stages"]
            self.current_stage_num = data["current_stage_num"]
            self.num_stages = data["num_stages"]
            self.attempts_allowed = data["attempts_allowed"]
            return
        if stage_handler is not None:
            initial_stage = stage_handler.generate_initial_stage()
            self.stages = [initial_stage]
            self.current_stage_num = 0
            self.num_stages = 1  # Adjust as needed
            self.attempts_allowed = 3  # Default value, adjust as needed
            return
        raise ValueError("Must provide either a stage handler or a json string")

    def set_stages_current(self):
        cur_stage = self.stages[self.current_stage_num]
        cur_stage['is_current'] = True
        cur_stage['is_upcoming'] = False

    def get_question_name(self, question_name: str) -> str:
        return f"{self.current_stage_num}-{question_name}"

    def set_stage_finished(self, stage):
        stage['is_finished'] = True
        stage['is_current'] = False

    def generate(self, data: QuestionData):
        print("inside generate")

    def grade(self, data: QuestionData, stage_handler: StageHandler):
        stage_handler.grade_current_stage(self.stages[self.current_stage_num], self.get_question_name("answer"), data, self.current_stage_num, self.num_stages, self.attempts_allowed)

    def __getstate__(self):
        return {
            "stages": self.stages,
            "current_stage_num": self.current_stage_num,
            "num_stages": self.num_stages,
            "attempts_allowed": self.attempts_allowed
        }

    def toJSON(self) -> str:
        return json.dumps(self.__getstate__())

# Update the generate and grade functions accordingly
