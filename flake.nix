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
        
        # Build the blog parser using Nix's Haskell infrastructure
        blogParser = pkgs.haskellPackages.callCabal2nix "blog-parser" ./blog-parser {};
        
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "portfolio-site";
          src = ./.;
          
          buildInputs = with pkgs; [ 
            nodejs
            nodePackages.npm
            blogParser  # The pre-built blog parser
          ];
          
          buildPhase = ''
            echo "Running blog parser to generate HTML files and blogs.json..."
            blog-parser -o frontend/public -t blogs -j frontend/public
            
            echo "Building frontend..."
            cd frontend

            export npm_config_cache=$TMPDIR/.npm
            export npm_config_strict_ssl=false
            export NODE_TLS_REJECT_UNAUTHORIZED=0

            npm ci --verbose
            npm run build --verbose
          '';
          
          installPhase = ''
            cp -r dist $out
          '';
        };
      });
}