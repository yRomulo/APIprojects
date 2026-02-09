import type { Request, Response } from "express";
import {
  registerService,
  loginService,
} from "./auth.service.js";
import type { LoginDto } from "./auth.types.js";
import type { CreateUserDTO } from "../users/user.types.js";

export const registerController = async (
  req: Request<unknown, unknown, CreateUserDTO>,
  res: Response
) => {
  try {
    const { name, email, password } = req.body;

    if (!name || !email || !password) {
      return res.status(400).json({
        message: "Missing required fields: name, email, password",
      });
    }

    const user = await registerService({
      name,
      email,
      password,
    });

    return res.status(201).json({
      message: "User registered successfully",
      user,
    });
  } catch (error) {
    if (error instanceof Error && error.message === "User already exists") {
      return res.status(409).json({
        message: "Email already registered",
      });
    }

    return res.status(500).json({
      message: "Error registering user",
      error: error instanceof Error ? error.message : error,
    });
  }
};

export const loginController = async (
  req: Request<unknown, unknown, LoginDto>,
  res: Response
) => {
  try {
    const { email, password } = req.body;

    if (!email || !password) {
      return res.status(400).json({
        message: "Missing required fields: email, password",
      });
    }

    const result = await loginService({
      email,
      password,
    });

    return res.status(200).json({
      message: "Login successful",
      token: result.token,
      user: result.user,
    });
  } catch (error) {
    if (error instanceof Error && error.message === "Invalid credentials") {
      return res.status(401).json({
        message: "Invalid email or password",
      });
    }

    return res.status(500).json({
      message: "Error logging in",
      error: error instanceof Error ? error.message : error,
    });
  }
};
