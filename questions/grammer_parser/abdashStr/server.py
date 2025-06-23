import regex_rrg_instructions

def generate(data):
    data["params"]["regex_rrg_instructions"] = regex_rrg_instructions.render(data, {
            "regex_file": "abdashStr_regexp",
            "rrg_file": "abdashStr_rrg"
        })