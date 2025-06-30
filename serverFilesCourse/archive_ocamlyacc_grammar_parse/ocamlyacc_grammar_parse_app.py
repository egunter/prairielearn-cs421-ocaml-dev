import chevron

def render(data, params):
    with open((data["options"]["server_files_course_path"])+"/ocamlyacc_grammar_parse_app.mustache") as f:
        return chevron.render(f, params)
