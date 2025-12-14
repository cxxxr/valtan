{
  description = "Valtan - Common Lisp to JavaScript compiler";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      perSystem =
        { pkgs, system, ... }:
        {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              sbcl
              nodejs_20
              nodePackages.npm
              curl
              # For iolib/inotify
              libfixposix
              gcc
              pkg-config
            ];

            shellHook = ''
              export CL_SOURCE_REGISTRY="$PWD//:$PWD/library/valtan-core//:$PWD/submodules/cl-source-map//"
              export C_INCLUDE_PATH="${pkgs.libfixposix}/include:$C_INCLUDE_PATH"
              export LIBRARY_PATH="${pkgs.libfixposix}/lib:$LIBRARY_PATH"
              export LD_LIBRARY_PATH="${pkgs.libfixposix}/lib:$LD_LIBRARY_PATH"

              echo "========================================"
              echo "Valtan development shell"
              echo "========================================"
              echo ""
              echo "Build valtan CLI:"
              echo "  sbcl --load ~/.quicklisp/setup.lisp \\"
              echo "       --eval '(push (pathname \"$PWD/\") asdf:*central-registry*)' \\"
              echo "       --eval '(push (pathname \"$PWD/library/valtan-core/\") asdf:*central-registry*)' \\"
              echo "       --eval '(push (pathname \"$PWD/submodules/cl-source-map/\") asdf:*central-registry*)' \\"
              echo "       --eval '(ql:quickload :valtan-cli)' \\"
              echo "       --eval '(asdf:make :valtan-cli/executable)' \\"
              echo "       --eval '(quit)'"
              echo ""
              echo "Run valtan (after build):"
              echo "  ./valtan build <system-file>"
              echo ""
            '';
          };
        };
    };
}
