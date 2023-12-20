# coffee-shop-backend
- Web framework - scotty
- Configuration - configurator
- DB connection - resourse-pool

# REST
## Authentication
- `POST /auth/getToken` - получить токен
- `POST /auth/register` - зарегистрироваться
- `GET  /auth/checkToken` - проверить токен

## User
- `GET /user` - информация о текущем пользователе

## Orders
- `POST  /orders` - создать заказ
- `GET   /orders` - заказы текущего пользователя
- `GET   /orders/{id}` - информация о заказе
- `POST  /orders/{id}/cancel` - отменить заказ

## Menu
- `GET /coffeeShops` - доступные кофейни
- `GET /coffeeShops/{id}` - информация о кофейне
- `GET /coffeeShops/{id}/items` - товары в кофейне
- `GET /coffeeShops/{id}/items/{id}` - информация о товаре

## Admin
- `GET    /admin/coffeeShops` - доступные кофейни организации
- `GET    /admin/coffeeShops/{id}`
- `GET    /admin/coffeeShops/{id}/items`
- `GET    /admin/coffeeShops/{id}/items/{id}`
- `POST   /admin/coffeeShops`
- `PATCH  /admin/coffeeShops/{id}`
- `DELETE /admin/coffeeShops/{id}`
- `POST   /admin/coffeeShops/{id}/items`
- `PATCH  /admin/coffeeShops/{id}/items/{id}`
- `DELETE /admin/coffeeShops/{id}/items/{id}`
- `GET    /admin/coffeeShops/{id}/orders`
- `PATCH  /admin/coffeeShops/{id}/orders/{id}`
