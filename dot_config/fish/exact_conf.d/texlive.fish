# Homebrew's standalone dvisvgm bundles its own kpathsea, which searches
# relative to its own prefix and never finds the texlive formula's tree.
# Point it there explicitly.  No-op when the directory is absent (MacTeX
# installs dvisvgm inside its own tree and needs none of this).
if test -d /opt/homebrew/opt/texlive/share/texmf-dist/web2c; and not set -q TEXMFCNF
    set -gx TEXMFCNF /opt/homebrew/opt/texlive/share/texmf-dist/web2c
    set -gx TEXMFROOT /opt/homebrew/opt/texlive/share
    set -gx TEXMFDIST /opt/homebrew/opt/texlive/share/texmf-dist
end
