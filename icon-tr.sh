#!/bin/bash

# fa -> feather
# icon-circle-o -> icon role ?
# icon-circle -> icon role ?
rg src/ -l -e "icon-share-square" | xargs sed -i "s/icon-share-square/icon-share/g"
rg src/ -l -e "icon-clone" | xargs sed -i "s/icon-clone/icon-copy/g"
rg src/ -l -e "icon-times" | xargs sed -i "s/icon-times/icon-x/g"
rg src/ -l -e "icon-square-o" | xargs sed -i "s/icon-square-o/icon-square/g"
rg src/ -l -e "icon-pencil" | xargs sed -i "s/icon-pencil/icon-pen/g"
rg src/ -l -e "icon-cog" | xargs sed -i "s/icon-cog/icon-settings/g"
rg src/ -l -e "icon-comments" | xargs sed -i "s/icon-comments/icon-message-square/g"
rg src/ -l -e "icon-scroll" | xargs sed -i "s/icon-book-open/icon-book-open/g"
#rg src/ -l -e "icon-ellipsis" | xargs sed -i "s/icon-ellipsis/icon-more-hozirontal/g"
#rg src/ -l -e "icon-ellipsis-v" | xargs sed -i "s/icon-ellipsis-v/icon-more-vertical/g"
rg src/ -l -e "icon-sign-in-alt" | xargs sed -i "s/icon-sign-in/icon-log-in/g"
rg src/ -l -e "icon-sign-out-alt" | xargs sed -i "s/icon-sign-out/icon-log-out/g"
rg src/ -l -e "icon-sort-amount-up" | xargs sed -i "s/icon-sort-amount-up/icon-sort-amount-desc/g"
