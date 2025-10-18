set quiet
set dotenv-load

work_dir := `pwd`

default:
  just --list

generate-icons:
  #!/usr/bin/env bash

  # Configuration
  cd ./assets/images/logo/
  SVG_SOURCE="logo.svg"
  BG_COLOR="white"

  if [ ! -f "$SVG_SOURCE" ]; then
      echo "Error: $SVG_SOURCE not found!"
      exit 1
  fi

  echo "🎨 Generating icons from $SVG_SOURCE..."

  # Favicon ICO (16x16, 32x32, 48x48) -- @deprecated
  #convert -background none "$SVG_SOURCE" -define icon:auto-resize=16,32,48 favicon.ico
  #echo "✓ favicon.ico"

  # Apple Touch Icon (180x180 with padding)
  convert "$SVG_SOURCE" -resize 160x160 \
    -background "$BG_COLOR" -gravity center -extent 180x180 \
    apple-touch-icon.png
  echo "✓ apple-touch-icon.png"

  # PWA Icons (192x192, 512x512)
  convert -background none -resize 192x192 "$SVG_SOURCE" icon-192.png
  convert -background none -resize 512x512 "$SVG_SOURCE" icon-512.png
  echo "✓ icon-192.png"
  echo "✓ icon-512.png"

  # Open Graph image
  # Note: generate a 1200x630 for image below the text (like github.com, checj telegram web bot)
  convert "$SVG_SOURCE" -resize 1000x1000 \
    -background "$BG_COLOR" -gravity center -extent 1200x1200 \
    og-image.png
  echo "✓ og-image.png"

  # For bimi; manually edit the svg to add to
  # - remove <styles>
  # - ensure: `<svg version="1.2" baseProfile="tiny"  >`


