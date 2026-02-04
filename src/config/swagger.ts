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
    },
  },

  // Caminho onde o swagger vai ler as docs
  apis: ["./src/modules/**/*.ts", "./src/docs/**/*.ts"],
};

export const swaggerSpec = swaggerJsdoc(swaggerOptions);
