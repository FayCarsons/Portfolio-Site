{
  description = "Portfolio site with blog parser";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "portfolio-site";
          src = ./.;
          
          buildInputs = with pkgs; [ 
            ghc 
            cabal-install 
            nodejs
            nodePackages.npm
            # System dependencies for Haskell packages
            zlib
            zlib.dev
            pkg-config
          ];
          
          # Make sure pkg-config can find the libraries
          PKG_CONFIG_PATH = "${pkgs.zlib.dev}/lib/pkgconfig";
          
          buildPhase = ''
            # Set up temporary home directory for cabal
            export HOME=$TMPDIR
            mkdir -p $HOME/.cabal
            
            echo "Building blog parser..."
            cd blog-parser
            
            # Initialize cabal in the temporary home
            cabal user-config init
            cabal update
            cabal configure
            cabal build
            cd ..
            
            echo "Building frontend..."
            cd frontend
            
            # Set npm cache to temporary directory
            export npm_config_cache=$TMPDIR/.npm
            npm ci
            npm run build
          '';
          
          installPhase = ''
            cp -r frontend/dist $out
          '';
        };
        
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [ 
            ghc 
            cabal-install 
            nodejs 
            nodePackages.npm
            zlib
            zlib.dev
            pkg-config
          ];
        };
      });
}