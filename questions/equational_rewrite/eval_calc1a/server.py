import rewrite_evaluation_template_instructions

def generate(data):
    data["params"]["rewrite_evaluation_instructions"] = rewrite_evaluation_template_instructions.render(data, {
        "question_name": "eval_calc1a",
        })
