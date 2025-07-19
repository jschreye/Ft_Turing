#!/bin/bash

# === 1. Préparation des dossiers ===
mkdir -p "$HOME/opt/bin"

# === 2. Télécharger OPAM binaire dans ~/opt/bin ===
echo "🔽 Téléchargement d'OPAM..."
curl -fsSL https://github.com/ocaml/opam/releases/download/2.1.5/opam-2.1.5-x86_64-linux -o "$HOME/opt/bin/opam"
chmod +x "$HOME/opt/bin/opam"

# === 3. Ajouter ~/opt/bin au PATH (si pas déjà fait) ===
if ! grep -q 'export PATH="$HOME/opt/bin:$PATH"' "$HOME/.zshrc"; then
    echo 'export PATH="$HOME/opt/bin:$PATH"' >> "$HOME/.zshrc"
    echo 'eval $(opam env)' >> "$HOME/.zshrc"
    echo "✅ PATH mis à jour dans ~/.zshrc"
fi

# === 4. Charger le nouveau PATH maintenant ===
export PATH="$HOME/opt/bin:$PATH"

# === 5. Initialisation d'OPAM ===
echo "⚙️ Initialisation d'OPAM..."
opam init --bare --disable-sandboxing -y

# === 6. Créer le switch OCaml 5.1.1 ===
echo "🔧 Création du switch OCaml 5.1.1..."
opam switch create 5.1.1 -y

# === 7. Activer le switch dans ce shell ===
eval $(opam env)

# === 8. Installer les dépendances du projet ===
echo "📦 Installation des paquets dune et yojson..."
opam install dune yojson -y

echo "✅ Installation terminée. Redémarre le terminal ou fais : source ~/.zshrc"