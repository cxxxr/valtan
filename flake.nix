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
              export PATH="$PWD:$PATH"

              if [ ! -x "$PWD/valtan" ]; then
                echo "========================================"
                echo "Valtan development shell"
                echo "========================================"
                echo ""
                echo "valtan CLI not found. Build it with:"
                echo "  sbcl --load ~/.quicklisp/setup.lisp \\"
                echo "       --eval '(ql:quickload :valtan-cli)' \\"
                echo "       --eval '(asdf:make :valtan-cli/executable)' \\"
                echo "       --eval '(quit)'"
                echo ""
              fi
            '';
          };
        };
    };
}
