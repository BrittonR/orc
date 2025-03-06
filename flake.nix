{
  description = "Gleam development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "gleam-dev-environment";
          
          buildInputs = with pkgs; [
            # Gleam compiler and tools
            gleam
            
            # Erlang/OTP
            erlang
            erlang-ls # Erlang Language Server
            rebar3     # Erlang build tool
            
            # Elixir (sometimes used with Gleam)
            elixir
            elixir-ls
            
            # Additional tools
            inotify-tools # For file watching (useful for tests)
            
            # Development helpers
            pkg-config
            openssl
            gnumake
            gcc
          ];
          
          shellHook = ''
            echo "Gleam development environment loaded!"
            echo "Gleam version: $(gleam --version)"
            echo "Erlang version: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
            echo "Elixir version: $(elixir --version)"
          '';
        };
      }
    );
}
