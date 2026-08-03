(lang dune 3.0)

(generate_opam_files true)

(maintainers "TODO")

(authors "TODO")

(license MIT)

(package
 (name execlab)
 (synopsis "A historical execution benchmarking laboratory")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  ocaml-lsp-server
  (ocamlformat
   (and
    (>= 0.26.2)
    (< 0.27)))
  (odoc :with-doc)
  core
  core_unix
  ppx_jane
  (expect_test_helpers_core :with-test)))
