# mat-twitter-normalized-distance
The visualization of Normalized social distance data from [mat-twitter-downloader](https://github.com/zabkwak/mat-twitter-downloader). It's based on shiny app.

## Development
### Requirements
- R (Rscript)
#### Ubuntu
- libglu1
- libxml2
- libbglpk-dev
- libxt-dev

### Installation & test run
```bash
git clone git@github.com:zabkwak/mat-twitter-normalized-distance.git
cd mat-twitter-normalized-distance
Rscript -e "install.packages(c('shiny', 'shinythemes', 'rgl', 'psych', 'rjson', 'ca', 'nFactors', 'ape', 'wordcloud', 'qgraph', 'polycor', 'bipartite', 'FactoMineR', 'png', 'vegan3d', 'ade4', 'vegan', 'fpc', 'cluster', 'network', 'corrplot', 'smacof', 'e1071', 'mclust', 'MASS', 'ape', 'igraph', 'plotrix'))"
Rscript main.R
```
The app will be accessible on `http://localhost:8080`.

### Dockerfile
Make sure all new packages are installed in `Dockerfile` with `RUN install2.r --error [package]`.

## Settings
No additional configuration is needed.

## Docker
The [image](https://github.com/zabkwak/mat-twitter-normalized-distance/pkgs/container/mat-twitter-normalized-distance) is stored in GitHub packages registry and the app can be run in the docker environment.
```bash
docker pull ghcr.io/zabkwak/mat-twitter-normalized-distance:latest
```

```bash
docker run \
-p 8080:8080 \
-d \
--restart=unless-stopped \
--name=mat-twitter-normalized-distance \
ghcr.io/zabkwak/mat-twitter-normalized-distance:latest  
```
The app will be accessible on `http://localhost:8080`.

*This work was supported by the European Regional Development Fund-Project “Creativity and Adaptability as Conditions of the Success of Europe in an Interrelated World” (No. CZ.02.1.01/0.0/0.0/16_019/0000734)."*
