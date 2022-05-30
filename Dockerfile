FROM rocker/r-ver:latest

ARG NPM_GITHUB_READ
ENV NPM_GITHUB_READ=$NPM_GITHUB_READ
WORKDIR /usr/src/app
COPY . .

RUN apt-get update && apt-get -y install libglu1 libxml2 libglpk-dev libxt-dev

RUN install2.r --error shiny
RUN install2.r --error shinythemes
RUN install2.r --error rgl
RUN install2.r --error psych
RUN install2.r --error rjson
RUN install2.r --error ca
RUN install2.r --error nFactors
RUN install2.r --error ape
RUN install2.r --error wordcloud
RUN install2.r --error qgraph
RUN install2.r --error polycor
RUN install2.r --error bipartite
RUN install2.r --error FactoMineR
RUN install2.r --error png
RUN install2.r --error vegan3d
RUN install2.r --error ade4
RUN install2.r --error vegan
RUN install2.r --error fpc
RUN install2.r --error cluster
RUN install2.r --error network
RUN install2.r --error corrplot
RUN install2.r --error smacof
RUN install2.r --error e1071
RUN install2.r --error mclust
RUN install2.r --error MASS
RUN install2.r --error ape
RUN install2.r --error igraph
RUN install2.r --error plotrix

ENV PORT=8080
LABEL org.opencontainers.image.source https://github.com/zabkwak/mat-twitter-normalized-distance
EXPOSE 8080

CMD [ "Rscript", "main.R" ]
