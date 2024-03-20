{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "8d3ed79ae8875587a2bdcb9d3cbb445fcfbbf5ce";
  }) {
    inherit pkgs;
    verbosity = "info";
  };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };
}
