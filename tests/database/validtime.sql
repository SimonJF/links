DROP TABLE IF EXISTS employees;
DROP TABLE IF EXISTS positions;

CREATE TABLE employees (
    "name" text,
    "salary" integer,
    "position_id" integer,
    "valid_from" timestamp with time zone,
    "valid_to" timestamp with time zone
);

CREATE TABLE positions (
  "position_id" SERIAL,
  "position" text
);
