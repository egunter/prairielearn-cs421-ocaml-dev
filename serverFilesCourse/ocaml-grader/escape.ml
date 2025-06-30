let _ = 
  let in_strm = open_in "/grade/run/tmp_unescaped" in
  let out_strm = open_out "tmp_escaped" in
  let _ = output_string out_strm "\"output\":\"" in
  let rec input_output_string in_strm =
    try 
      let _ = output_string out_strm ((String.escaped (input_line in_strm)) ^ "\\n" )
      in input_output_string in_strm 
    with End_of_file -> 
          let _ = output_string out_strm "\", " in
          let _ = close_in in_strm in
          let _ = flush out_strm in
          close_out out_strm
  in input_output_string in_strm

