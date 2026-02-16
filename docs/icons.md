# Fractale Icons

To see all the available icon, do `cat assets/icons/style.css | grep "\.icon[^ :]*" -o`

The icon can used like this

```css
<span class="icon-pin"></span>
```

In Elm, we do

```elm
import Assets as A

A.icon "icon-edit-2"
```


## Generate icons

Download the icons from the fractale project in https://icomoon.io/

Then replace the icons with the following commands

```bash
cd assets/icons
rm -rf demo.html fonts demo-files/ Read\ Me.txt  selection.json  style.css
cp ~/Downloads/fractaleicon-v1.0.zip . && unzip fractaleicon-v1.0.zip && rm fractaleicon-v1.0.zip
```

The icomoon project backup file is located in `../_extra/fractale-icons.json`.
