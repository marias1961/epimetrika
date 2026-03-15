# ── Stage 1: Build ───────────────────────────────────────────────────────────
FROM node:20-alpine AS builder
WORKDIR /app

# Instalar dependencias primero (capa cacheada mientras package.json no cambie)
COPY package*.json ./
RUN npm ci --prefer-offline

# Copiar el resto y compilar
COPY . .
RUN npm run build

# ── Stage 2: Serve with Nginx ────────────────────────────────────────────────
FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
