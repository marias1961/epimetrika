FROM nginx:alpine
# Copiamos la build compilada localmente en la máquina host
COPY dist /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
