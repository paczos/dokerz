docker build --rm -t 42n4/rstudio .

https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image

https://github.com/rocker-org/rocker-versioned/tree/master/rstudio

docker run -d -p 8787:8787 -e USER=guest -e USERID=1001 -e GROUPID=100 -v $(pwd):/home/guest 42n4/rstudio
