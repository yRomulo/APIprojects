import swaggerJsdoc from "swagger-jsdoc";
import { env } from "./env.js";
import { schemas } from "../docs/schemas.js";

const swaggerOptions: swaggerJsdoc.Options = {
  definition: {
    openapi: "3.0.0",
    info: {
      title: "API Projects",
      version: "1.0.0",
      description: "API para gerenciamento de usuários e projetos",
    },
    servers: [
      {
        url: `http://localhost:${env.PORT}/api`,
      },
    ],
    components: {
      schemas,
      securitySchemes: {
        bearerAuth: {
          type: "http",
          scheme: "bearer",
          bearerFormat: "JWT",
          description:
            "JWT Authorization header usando o Bearer scheme. Ex: Authorization: Bearer {token}",
        },
      },
    },
  },

  // Caminho onde o swagger vai ler as docs
  apis: ["./src/modules/**/*.ts", "./src/docs/**/*.ts"],
};

export const swaggerSpec = swaggerJsdoc(swaggerOptions);
