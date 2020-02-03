## Dockerfile for a haskell environment
FROM hmemcpy/hie:8.6.5
RUN nix-env -iA nixpkgs.stack
RUN nix-env -iA nixpkgs.bashInteractive
RUN nix-env -iA nixpkgs.binutils
RUN ln -s /root/.nix-profile/bin/bash /bin/bash
