{
  description = "Valtan - Common Lisp to JavaScript compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # Development shell with all tools needed to build valtan
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.sbcl
            pkgs.nodejs_20
            pkgs.nodePackages.npm
            pkgs.curl
          ];

          shellHook = ''
            export CL_SOURCE_REGISTRY="$PWD//:$PWD/library/valtan-core//:$PWD/submodules/cl-source-map//"

            echo "========================================"
            echo "Valtan development shell"
            echo "========================================"
            echo ""
            echo "To build valtan with Quicklisp:"
            echo ""
            echo "  # First time setup:"
            echo "  curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp"
            echo "  sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'"
            echo ""
            echo "  # Build valtan:"
            echo "  sbcl --load ~/quicklisp/setup.lisp \\"
            echo "       --eval '(push (pathname \"$PWD/\") asdf:*central-registry*)' \\"
            echo "       --eval '(push (pathname \"$PWD/library/valtan-core/\") asdf:*central-registry*)' \\"
            echo "       --eval '(push (pathname \"$PWD/submodules/cl-source-map/\") asdf:*central-registry*)' \\"
            echo "       --eval '(ql:quickload :valtan-cli)' \\"
            echo "       --eval '(asdf:make :valtan-cli/executable)' \\"
            echo "       --eval '(quit)'"
            echo ""
          '';
        };
      }
    );
}
