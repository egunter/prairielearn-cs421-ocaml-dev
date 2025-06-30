# Picoml Environment Question Documentation

## Overview

The **Picoml Environment Question** evaluates a student's ability to understand and represent environments in a functional programming context. 

Each question stage provides a code snippet, and students are required to input the resulting environment after the execution of the given declarations. 

---

## Aims

1. Assess a student's ability to work with environments in a functional programming language.
2. Teach syntax for representing closures and nested environments.
3. Provide a multi-stage interactive experience with feedback.

---

## Features

| Feature                                  | Description                                                                                     |
|------------------------------------------|-------------------------------------------------------------------------------------------------|
| Multi-Stage Questions                    | The question progresses through multiple stages, each building on the previous one.            |
| Environment Validation                   | Validates student-submitted environments against expected answers while ignoring binding order. |
| Syntax Feedback                          | Detects and notifies students of invalid environment syntax without deducting attempts.         |
| Attempt Tracking                         | Tracks and limits attempts per stage.         |

---


### How to Use the Question Framework


 **Modify Stages**:
   - Edit the `self.stages` array in the `PicomlEnvHandler` class to change the sequence of code snippets and their corresponding correct environments.

---


#### Picoml Environment Handler (`PicomlEnvHandler`)



| Property/Method               | Description                                                                                   |
|--------------------------------|-----------------------------------------------------------------------------------------------|
| `stages`                       | Array defining each stage's code snippet and the corresponding correct environment.           |
| `generate_initial_stage`       | Sets up the initial question stage and initializes necessary data structures.                |
| `generate_next_stage`          | Prepares the next stage in the question flow after a correct submission or max attempts.      |
| `grade_current_stage`          | Grades the student’s current submission and provides feedback.                      |
| `parse_environment`            | Parses the student-submitted environment into a dictionary for validation.                   |
| `environments_equal`           | Compares two environments for equality, ignoring order and validating nested environments.    |
| `closures_equal`               | Validates equivalency of closures by comparing function bodies and their environments.        |
| `toJSON` / `fromJSON`          | Supports serialization and deserialization of question data for persistent storage.          |



---


This `TypedDict` defines the structure of each stage, specifying key elements such as the code snippet, correct environment, and the grading status.

| Property         | Type    | Description                                                                 |
|-------------------|---------|-----------------------------------------------------------------------------|
| `code`           | `str`   | The code snippet for the current stage.                                     |
| `correct_env`    | `str`   | The correct environment after executing the code.                          |
| `student_answer` | `str`   | The environment submitted by the student.                                  |
| `is_correct`     | `bool`  | Indicates whether the student's answer is correct.                         |
| `attempts_left`  | `int`   | The number of attempts left for the current stage.                         |


---

## How to interact with the interface?

1. **Input Your Answer**

- Type the environment in the provided text box, ensuring it follows the specified syntax guidelines.
 

2. **Submitting Your Answer**

- Click the **"Save and Grade"** button to submit your response.
- Your score will partially update for each stage you answer correctly.

3. **Managing Attempts**
- You are allowed a limited number of attempts to answer each stage.
- If you exhaust all attempts it will automatically move to the next stage.

4. **Handle Invalid Syntax**
- Submissions with invalid syntax will **not count as an attempt**.

5. **Viewing Correct Answers**

- After advancing to the next stage, the correct environment for the previous stage will be displayed.


---

**Example Initialisation**

```py
def __init__(self):
        self.stages = [
            {
                'code': 'let a = 5;;',
                'correct_env': '{a -> 5}'
            },
            {
                'code': 'let c = 7 + a;;',
                'correct_env': '{a -> 5, c -> 12}'
            },
            {
                'code': 'let f y = y + c;;',
                'correct_env': '{a -> 5, c -> 12, f -> <y -> y + c, {a -> 5, c -> 12}>}'
            },
            {
                'code': 'let g u v = f (c + u - v);;',
                'correct_env': '{a -> 5, c -> 12, f -> <y -> y + c, {a -> 5, c -> 12} >, g -> <u -> fun v -> f (c + u - v), {a -> 5, c -> 12, f -> <y -> y + c, {a -> 5, c -> 12}>}>}'
            },
            {
                'code': 'let c = 1;;',
                'correct_env': '{a -> 5, c -> 1, f -> <y -> y + c, {a -> 5, c -> 12} >, g -> <u -> fun v -> f (c + u - v), {a -> 5, c -> 12, f -> <y -> y + c, {a -> 5, c -> 12}>}>}'
            },
            {
                'code': '''let b = (f c)
  in
  (let a = 5
    in
    a + b + c );;''',
                'correct_env': '{a -> 5, c -> 1, f -> <y -> y + c, {a -> 5, c -> 12} >, g -> <u -> fun v -> f (c + u - v), {a -> 5, c -> 12, f -> <y -> y + c, {a -> 5, c -> 12}>}>}'
            },
            {
                'code': 'let f = f c;;',
                'correct_env': '{a -> 5, c -> 1, f -> 13, g -> <u -> fun v -> f (c + u - v), {a -> 5, c -> 12, f -> <y -> y + c, {a -> 5, c -> 12}>}>}'
            }
        ]
        self.num_stages = len(self.stages)
        self.stages_info: list[PicomlStageInfo] = []
        self.attempts_allowed = 3
```