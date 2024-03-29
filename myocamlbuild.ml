(* OASIS_START *)
(* DO NOT EDIT (digest: 956f5288d32a7a1c8abb0d0365a3324e) *)
module OASISGettext = struct
# 21 "/media/data/ocaml/oasis/src/oasis/OASISGettext.ml"
  
  let ns_ str =
    str
  
  let s_ str =
    str
  
  let f_ (str : ('a, 'b, 'c, 'd) format4) =
    str
  
  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""
  
  let init =
    []
  
end

module OASISExpr = struct
# 21 "/media/data/ocaml/oasis/src/oasis/OASISExpr.ml"
  
  
  
  open OASISGettext
  
  type test = string 
  
  type flag = string 
  
  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string
    
  
  type 'a choices = (t * 'a) list 
  
  let eval var_get t =
    let rec eval' =
      function
        | EBool b ->
            b
  
        | ENot e ->
            not (eval' e)
  
        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)
  
        | EOr (e1, e2) ->
            (eval' e1) || (eval' e2)
  
        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")
  
        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t
  
  let choose ?printer ?name var_get lst =
    let rec choose_aux =
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then
              vl
            else
              choose_aux tl
        | [] ->
            let str_lst =
              if lst = [] then
                s_ "<empty>"
              else
                String.concat
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with
                | Some nm ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)
  
end


module BaseEnvLight = struct
# 21 "/media/data/ocaml/oasis/src/base/BaseEnvLight.ml"
  
  module MapString = Map.Make(String)
  
  type t = string MapString.t
  
  let default_filename =
    Filename.concat
      (Sys.getcwd ())
      "setup.data"
  
  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line =
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer =
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer;
                Stream.junk lexer;
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith
          (Printf.sprintf
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end
  
  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute
          buff
          (fun var ->
             try
               var_expand (MapString.find var env)
             with Not_found ->
               failwith
                 (Printf.sprintf
                    "No variable %s defined when trying to expand %S."
                    var
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)
  
  let var_choose lst env =
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


module MyOCamlbuildFindlib = struct
# 21 "/media/data/ocaml/oasis/src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml"
  
  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin
  
  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read
  
  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings
  
  let split s ch =
    let x = 
      ref [] 
    in
    let rec go s =
      let pos = 
        String.index s ch 
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x
  
  let split_nl s = split s '\n'
  
  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s
  
  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")
  
  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]
  
  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]
  
  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());
  
          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());
  
          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])
  
      | _ -> 
          ()
  
end

module MyOCamlbuildBase = struct
# 21 "/media/data/ocaml/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"
  
  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)
  
  
  
  open Ocamlbuild_plugin
  module OC = Ocamlbuild_pack.Ocaml_compiler
  
  type dir = string 
  type file = string 
  type name = string 
  type tag = string 
  
# 56 "/media/data/ocaml/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"
  
  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir * file list) list; 
        flags:     (tag list * (spec OASISExpr.choices)) list;
        (* Replace the 'dir: include' from _tags by a precise interdepends in
         * directory.
         *)
        includes:  (dir * dir list) list; 
      } 
  
  let env_filename =
    Pathname.basename 
      BaseEnvLight.default_filename
  
  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 
  
  let tag_libstubs nm =
    "use_lib"^nm^"_stubs"
  
  let nm_libstubs nm =
    nm^"_stubs"
  
  let dispatch t e = 
    let env = 
      BaseEnvLight.load 
        ~filename:env_filename 
        ~allow_empty:true
        ()
    in
      match e with 
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try 
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]
  
        | Before_rules ->
          (* TODO: move this into its own file and conditionnaly include it, if
           * needed.
           *)
          (* OCaml cmxs rules: cmxs available in ocamlopt but not ocamlbuild.
             Copied from ocaml_specific.ml in ocamlbuild sources. *)
          let has_native_dynlink =
            try
              bool_of_string (BaseEnvLight.var_get "native_dynlink" env)
            with Not_found ->
              false
          in
          if has_native_dynlink && String.sub Sys.ocaml_version 0 4 = "3.11" then
            begin
              let ext_lib = !Options.ext_lib in
              let ext_obj = !Options.ext_obj in
              let ext_dll = !Options.ext_dll in
              let x_o = "%"-.-ext_obj in
              let x_a = "%"-.-ext_lib in
              let x_dll = "%"-.-ext_dll in
              let x_p_o = "%.p"-.-ext_obj in
              let x_p_a = "%.p"-.-ext_lib in
              let x_p_dll = "%.p"-.-ext_dll in
  
              rule "ocaml: mldylib & p.cmx* & p.o* -> p.cmxs & p.so"
                   ~tags:["ocaml"; "native"; "profile"; "shared"; "library"]
                   ~prods:["%.p.cmxs"; x_p_dll]
                   ~dep:"%.mldylib"
                   (OC.native_profile_shared_library_link_mldylib
                      "%.mldylib" "%.p.cmxs");
  
              rule "ocaml: mldylib & cmx* & o* -> cmxs & so"
                   ~tags:["ocaml"; "native"; "shared"; "library"]
                   ~prods:["%.cmxs"; x_dll]
                   ~dep:"%.mldylib"
                   (OC.native_shared_library_link_mldylib
                      "%.mldylib" "%.cmxs");
  
              rule "ocaml: p.cmx & p.o -> p.cmxs & p.so"
                   ~tags:["ocaml"; "native"; "profile"; "shared"; "library"]
                   ~prods:["%.p.cmxs"; x_p_dll]
                   ~deps:["%.p.cmx"; x_p_o]
                   (OC.native_shared_library_link ~tags:["profile"]
                                                  "%.p.cmx" "%.p.cmxs");
  
              rule "ocaml: p.cmxa & p.a -> p.cmxs & p.so"
                   ~tags:["ocaml"; "native"; "profile"; "shared"; "library"]
                   ~prods:["%.p.cmxs"; x_p_dll]
                   ~deps:["%.p.cmxa"; x_p_a]
                   (OC.native_shared_library_link ~tags:["profile"; "linkall"]
                                                  "%.p.cmxa" "%.p.cmxs");
  
              rule "ocaml: cmx & o -> cmxs"
                   ~tags:["ocaml"; "native"; "shared"; "library"]
                   ~prods:["%.cmxs"]
                   ~deps:["%.cmx"; x_o]
                   (OC.native_shared_library_link "%.cmx" "%.cmxs");
  
              rule "ocaml: cmx & o -> cmxs & so"
                   ~tags:["ocaml"; "native"; "shared"; "library"]
                   ~prods:["%.cmxs"; x_dll]
                   ~deps:["%.cmx"; x_o]
                   (OC.native_shared_library_link "%.cmx" "%.cmxs");
  
              rule "ocaml: cmxa & a -> cmxs & so"
                   ~tags:["ocaml"; "native"; "shared"; "library"]
                   ~prods:["%.cmxs"; x_dll]
                   ~deps:["%.cmxa"; x_a]
                   (OC.native_shared_library_link ~tags:["linkall"]
                                                  "%.cmxa" "%.cmxs");
            end
  
        | After_rules -> 
            (* Declare OCaml libraries *)
            List.iter 
              (function
                 | nm, [] ->
                     ocaml_lib nm
                 | nm, dir :: tl ->
                     ocaml_lib ~dir:dir (dir^"/"^nm);
                     List.iter 
                       (fun dir -> 
                          List.iter
                            (fun str ->
                               flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                            ["compile"; "infer_interface"; "doc"])
                       tl)
              t.lib_ocaml;
  
            (* Declare directories dependencies, replace "include" in _tags. *)
            List.iter 
              (fun (dir, include_dirs) ->
                 Pathname.define_context dir include_dirs)
              t.includes;
  
            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("-l"^(nm_libstubs lib)); A"-cclib";
                        A("-l"^(nm_libstubs lib))]);
  
                   flag ["link"; "library"; "ocaml"; "native"; tag_libstubs lib]
                     (S[A"-cclib"; A("-l"^(nm_libstubs lib))]);
                        
                   flag ["link"; "program"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("dll"^(nm_libstubs lib))]);
  
                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep  ["link"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];
  
                   dep  ["compile"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];
  
                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"] 
                     headers;
  
                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib] 
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;
  
              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec = 
                   BaseEnvLight.var_choose cond_specs env
                 in
                   flag tags & spec)
              t.flags
        | _ -> 
            ()
  
  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]
  
end


open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml = [("cv", ["src"])];
     lib_c = [("cv", "src/", [])];
     flags =
       [
          (["oasis_library_cv_ccopt"; "compile"],
            [
               (OASISExpr.EBool true,
                 S
                   [
                      A "-ccopt";
                      A "-g";
                      A "-ccopt";
                      A "-x";
                      A "-ccopt";
                      A "c++";
                      A "-ccopt";
                      A "-O2"
                   ])
            ]);
          (["oasis_library_cv_cclib"; "link"],
            [(OASISExpr.EBool true, S [A "-cclib"; A "-lstdc++"])]);
          (["oasis_library_cv_cclib"; "ocamlmklib"; "c"],
            [(OASISExpr.EBool true, S [A "-lstdc++"])])
       ];
     includes = [("test", ["src"])];
     }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

# 580 "myocamlbuild.ml"
(* OASIS_STOP *)
open Ocamlbuild_plugin

let () = Ocamlbuild_pack.Log.classic_display := true

let pkg_config flags package =
  with_temp_file "lwt" "pkg-config"
    (fun tmp ->
       Command.execute ~quiet:true & Cmd(S[A "pkg-config"; A("--" ^ flags); A package; Sh ">"; A tmp]);
       List.map (fun arg -> A arg) (string_list_of_file tmp))

let define_c_library ?(cplusplus=false) ~name ~c_name () =
  let tag = Printf.sprintf "use_C_%s" name in

  (* Get flags for using pkg-config: *)
  let opt = pkg_config "cflags" c_name and lib = pkg_config "libs" c_name in

  (* Add flags for linking with the C library: *)
  flag ["ocamlmklib"; "c"; tag] & S lib;

  (* flag ["ocamlmklib"; "c"] & S[A"-verbose"]; *)

  (* C stubs using the C library must be compiled with the library
     specifics flags: *)
  flag ["c"; "compile"; tag] & S(List.map (fun arg -> S[A"-ccopt"; arg]) opt);

  (* OCaml libraries must depends on the C library: *)
  flag ["link"; "ocaml"; tag] & S(List.map (fun arg -> S[A"-cclib"; arg]) lib);

  if cplusplus then flag ["c"; "compile"; tag] & S [A"-cc"; A"g++"]

let () = define_c_library ~cplusplus:false ~name:"opencv" ~c_name:"opencv" ()

let () = dispatch dispatch_default;;
