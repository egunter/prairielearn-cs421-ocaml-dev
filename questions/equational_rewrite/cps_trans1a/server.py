import cps_transformation_template_instructions

def generate(data):
    data["params"]["cps_transformation_instructions"] = cps_transformation_template_instructions.render(data, {
        "question_name": "cps_trans1a",
        })
