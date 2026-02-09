export const schemas = {
  /* ================= AUTH ================= */

  LoginDto: {
    type: "object",
    required: ["email", "password"],
    properties: {
      email: {
        type: "string",
        format: "email",
        example: "romulo@email.com",
      },
      password: {
        type: "string",
        format: "password",
        example: "123456",
      },
    },
  },

  LoginResponse: {
    type: "object",
    properties: {
      message: {
        type: "string",
        example: "Login successful",
      },
      token: {
        type: "string",
        example: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
      },
      user: {
        $ref: "#/components/schemas/UserResponse",
      },
    },
  },

  UserResponse: {
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
    },
  },

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
    required: ["title"],
    properties: {
      title: {
        type: "string",
        example: "Sistema ERP",
      },
      description: {
        type: "string",
        example: "Sistema de gestão empresarial",
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
      description: {
        type: "string",
        example: "Sistema de gestão empresarial",
      },
      status: {
        type: "string",
        enum: ["NEW", "IN_PROGRESS", "DONE"],
        example: "NEW",
      },
      userId: {
        type: "string",
        example: "clx123abc",
      },
      createdAt: {
        type: "string",
        format: "date-time",
      },
      updatedAt: {
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
      description: {
        type: "string",
        example: "Sistema de gestão empresarial atualizado",
      },
      status: {
        type: "string",
        enum: ["NEW", "IN_PROGRESS", "DONE"],
        example: "IN_PROGRESS",
      },
    },
  },
};
