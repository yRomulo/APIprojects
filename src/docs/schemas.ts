export const schemas = {
  /* ================= USERS ================= */

  CreateUserDTO: {
    type: "object",
    required: ["name", "email", "password"],
    properties: {
      name: {
        type: "string",
        example: "Rômulo Dias",
      },
      email: {
        type: "string",
        example: "romulo@email.com",
      },
      password: {
        type: "string",
        example: "123456",
      },
    },
  },

  User: {
    type: "object",
    properties: {
      id: {
        type: "string",
        example: "clx123abc",
      },
      name: {
        type: "string",
        example: "Rômulo Dias",
      },
      email: {
        type: "string",
        example: "romulo@email.com",
      },
      createdAt: {
        type: "string",
        format: "date-time",
      },
    },
  },

  UpdateUserDTO: {
    type: "object",
    properties: {
      name: {
        type: "string",
        example: "Rômulo Dias Atualizado",
      },
      email: {
        type: "string",
        example: "romulo.novo@email.com",
      },
      password: {
        type: "string",
        example: "novaSenha123",
      },
    },
  },

  /* ================= PROJECTS ================= */

  CreateProjectDTO: {
    type: "object",
    required: ["title", "userId"],
    properties: {
      title: {
        type: "string",
        example: "Sistema ERP",
      },
      userId: {
        type: "string",
        example: "clx123abc",
      },
    },
  },

  Project: {
    type: "object",
    properties: {
      id: {
        type: "string",
        example: "proj123",
      },
      title: {
        type: "string",
        example: "Sistema ERP",
      },
      userId: {
        type: "string",
        example: "clx123abc",
      },
      createdAt: {
        type: "string",
        format: "date-time",
      },
    },
  },

  UpdateProjectDTO: {
    type: "object",
    properties: {
      title: {
        type: "string",
        example: "Sistema ERP Atualizado",
      },
    },
  },
};
