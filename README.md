# spirit_island
Spirit Island score tracker and stat visualisation

## Record game results

![image](https://user-images.githubusercontent.com/17723393/224838456-ea3ad8c0-b048-42d4-8648-9f3b777e617d.png)

## Table of scores

![image](https://user-images.githubusercontent.com/17723393/224838480-b252d439-31bb-456c-979f-662e2295fbfa.png)

## Various plots

![image](https://user-images.githubusercontent.com/17723393/224838522-879dace2-5f89-4483-ab1a-fc17b30479c4.png)

![image](https://user-images.githubusercontent.com/17723393/224838585-2fc6d7cb-0afa-46a1-aa60-9c6fcd093c4f.png)

## Backup

Download your results to a csv file to browse and manipulate in any other way you want!

# Docker compose

Can host it with rocker/shiny-verse

```
shiny:
    image: rocker/shiny-verse
    restart: unless-stopped
    user: root
    command: >
      sh -c "Rscript /srv/shiny-server/spirit_island/install_packages.R &&
             chroot --userspec=shiny / /init" 
    volumes:
      - "/mnt/user/shiny/logs:/var/log/shiny-server"
      - "/mnt/user/shiny/mountpoints/apps:/srv/shiny-server"
```

You'll need to delete the `.Rprofile` file from the repository before you can launch it.