#!/bin/bash
# ci.sh

set -e

echo "🚀 Starting CI pipeline..."

# Форматирование файлов
echo "🔨 stylish-haskell..."
find ./ -name "*.hs" -exec sh -c 'for file do if ! grep -q -E -r "javascript_HOST_ARCH|LANGUAGE CPP|type \(\*\)" "$file"; then stylish-haskell --inplace "$file"; fi; done' sh {} \;

# Форматирование сборки
echo "🔨 cabal-fmt..."
cabal-fmt --inplace aeson-generic-ts.cabal

# Форматирование сборки
echo "🔨 gen-hie..."
gen-hie > hie.yaml

# Сборка
echo "🔨 Building..."
cabal build

# Тесты
echo "🧪 Testing..."
cabal test
# stack test --coverage
# stack hpc report --all an-exe.tix

# Линтинг
echo "📋 Linting..."
hlint -j .

echo "✅ CI pipeline completed successfully!"