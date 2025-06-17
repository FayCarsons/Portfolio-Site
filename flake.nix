{
  description = "Portfolio site with blog parser";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = nixpkgs.legacyPackages.${system};
        
        frontend = pkgs.buildNpmPackage {
          pname = "portfolio-frontend";
          version = "1.0.0";
          src = ./frontend;
          
          npmDepsHash = "sha256-UvZLVEnjejfj3W8/P+Vx9/B44Zldumt7aDsYiD95PkE=";
          
          buildPhase = ''
            npm run build
          '';
          
          installPhase = ''
            cp -r dist $out
          '';
        };
        
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "portfolio-site";
          src = ./.;
          
          buildInputs = with pkgs; [ 
            ghc 
            cabal-install 
            zlib
            zlib.dev
            pkg-config
          ];
          
          PKG_CONFIG_PATH = "${pkgs.zlib.dev}/lib/pkgconfig";
          
          buildPhase = ''
            export HOME=$TMPDIR
            mkdir -p $HOME/.cabal
            
            echo "Building blog parser..."
            cd blog-parser
            cabal user-config init
            cabal update
            cabal configure
            cabal build
            cd ..
          '';
          
          installPhase = ''
            # Copy the pre-built frontend
            cp -r ${frontend} $out
          '';
        };
      });
}