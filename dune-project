(lang dune 3.13)

(name signal)

(implicit_transitive_deps false)

(generate_opam_files true)

(source
 (github rizo/signal))

(authors "Rizo I. <rizo@odis.io>")

(maintainers "Rizo I. <rizo@odis.io>" "Leo Soares <leojrfs@gmail.com>")

(license MIT)

(documentation https://rizo.github.io/signal/signal)

(package
 (name signal)
 (synopsis "A small library for reactive signals")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  (odoc :with-doc)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.1)))))
