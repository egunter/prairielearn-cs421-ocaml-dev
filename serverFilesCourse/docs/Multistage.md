# Multi-Stage Question Framework

## Overview

The Multi-Stage Question Framework is a base module for handling multi-step question generation and grading. Most of the initial work included in this folder has been worked on by TheorieLearn Developers.

The aim of this framework is to allow instructors to create interactive question workflows with structured stages and real-time feedback.

### Aims

- To support a variety of multi-step questions in an extensible and modular manner.
- To provide a reusable and consistent framework for handling question logic across stages.


## Class Description

| Feature                     | Description                                                                                 |
|-----------------------------|---------------------------------------------------------------------------------------------|
| StageHandler Class          | Abstract base class defining methods for stage generation, grading, and state management.  |
| MultiStageQuestion Class    | Manages question stages and integrates grading with the UI.                                |
| Dynamic Question Generation | Supports question progression with multiple stages and feedback loops.                     |


## How to implement?

This folder serves as the base for creating and managing multi-stage questions. The goal is to provide a framework for developing questions or elements that can extend these classes to incorporate staged grading seamlessly.



## Steps to Create Multi-Stage Questions

1. **Extend the `StageHandler` Class**  
   - Create a custom class that inherits from `StageHandler`.  
   - Implement the following key methods to define the logic for your multi-stage question:
     - `generate_initial_stage`: Sets up the first stage of the question.
     - `generate_next_stage`: Defines how subsequent stages progress based on the current stage.
     - `grade_current_stage`: Implements the grading logic for the student's response at each stage.

   ### `StageHandler` Properties

   | **Property**       | **Type**                     | **Description**                                                                                 |
   |--------------------|------------------------------|-------------------------------------------------------------------------------------------------|
   | `generate_initial_stage` | Method                     | Initializes the first stage of the question.                                                    |
   | `generate_next_stage`    | Method                     | Defines logic to generate the next stage based on the current stage.                            |
   | `grade_current_stage`    | Method                     | Implements grading logic for the student’s response at each stage.                              |
   | `updateDisplay`         | Method                     | Updates the display parameters for the current stage in the front-end interface.                |
   | `toJSON`                | Method                     | Serializes the state of the handler to a JSON-compatible format.                                |
   | `fromJSON`              | Class Method               | Restores the state of the handler from a JSON-compatible format.                                |

2. **Configure the `MultiStageQuestion` Class**  
   - Use the `MultiStageQuestion` class to manage the flow of question stages.  
   - This class integrates with your custom `StageHandler` implementation to initialize, display, and grade the stages dynamically.

   ### `MultiStageQuestion` Properties

   | **Property**         | **Type**          | **Description**                                                                                   |
   |----------------------|-------------------|---------------------------------------------------------------------------------------------------|
   | `stages`             | `list[StageDict]` | Contains information about all stages, including their status and scores.                        |
   | `current_stage_num`  | `int`             | Tracks the index of the current active stage.                                                    |
   | `num_stages`         | `int`             | Total number of stages in the question.                                                          |
   | `attempts_allowed`   | `int`             | Number of attempts allowed for each stage.                                                       |
   | `generate`           | Method            | Initializes the question using a `StageHandler` implementation.                                  |
   | `grade`              | Method            | Grades the current stage and manages progression to subsequent stages.                           |
   | `toJSON`             | Method            | Serializes the question state into JSON format for storage or transmission.                      |
   | `fromJSON`           | Method            | Restores a question state from a JSON string.                                                    |


