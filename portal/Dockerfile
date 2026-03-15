# Etapa de construcción
FROM node:20-alpine AS builder
WORKDIR /app

# Copiar configuración de node
COPY package*.json ./
RUN npm install

# Copiar el resto y construir
COPY . .
RUN npm run build

# Etapa de producción (Sirviendo estáticos)
FROM nginx:alpine
# Copiar el build compilado
COPY --from=builder /app/dist /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
