CREATE TABLE "user" (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    user_role VARCHAR(20) NOT NULL,
    user_name VARCHAR(50) NOT NULL,
    user_last_name VARCHAR(50) NOT NULL,
    email VARCHAR(50) NOT NULL,
    phone VARCHAR(20)
);

CREATE TABLE coffee_shop (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    name VARCHAR(100) NOT NULL,
    address VARCHAR(100) NOT NULL,
    owner_id BIGINT REFERENCES "user"(id) NOT NULL,
    logo_url VARCHAR(100)
);

CREATE TABLE manager_info (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    user_id BIGINT REFERENCES  "user"(id) NOT NULL,
    coffee_shop_id BIGINT REFERENCES coffee_shop(id) NOT NULL
);

CREATE TABLE organization_info (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    user_id BIGINT REFERENCES "user"(id) NOT NULL,
    org_name VARCHAR(100)
);

CREATE TABLE credentials (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    user_id BIGINT REFERENCES "user"(id) NOT NULL,
    cred_type VARCHAR(100),
    cred_value VARCHAR(1000)
)
