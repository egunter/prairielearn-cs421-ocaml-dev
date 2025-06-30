import chevron

def render(data, params):
    with open((data["options"]["server_files_course_path"])+"/ocamlyacc_grammar_parse_try.mustache") as f:
        return chevron.render(f, params)
