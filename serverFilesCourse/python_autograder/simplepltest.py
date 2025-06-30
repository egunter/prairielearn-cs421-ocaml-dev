import json
import traceback


if __name__ == '__main__':
    try:
        from test import *

        grading_result = runTests()
        print(json.dumps(grading_result, allow_nan=False))

        with open('results.json', mode='w') as out:
            json.dump(grading_result, out)
    except:
        # Last-ditch effort to capture meaningful error information
        grading_result = {}
        grading_result['score'] = 0.0
        grading_result['succeeded'] = False
        grading_result['output'] = traceback.format_exc()

        with open('results.json', mode='w') as out:
            json.dump(grading_result, out)
