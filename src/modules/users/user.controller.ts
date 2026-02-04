import type { Request, Response } from "express";
import {
  createUserService,
  listUsersService,
  getUserByIdService,
  updateUserService,
  deleteUserService,
} from "./user.service.js";

export const createUserController = async (req: Request, res: Response) => {
  try {
    const user = await createUserService(req.body);
    return res.status(201).json(user);
  } catch (error) {
    return res.status(500).json({
      message: "Error creating user",
      error,
    });
  }
};

export const listUsersController = async (_req: Request, res: Response) => {
  try {
    const users = await listUsersService();
    return res.status(200).json(users);
  } catch (error) {
    return res.status(500).json({
      message: "Error listing users",
      error,
    });
  }
};

export const getUserByIdController = async (
  req: Request,
  res: Response
) => {
  try {
    const { userId } = req.params;

    if (!userId || typeof userId !== "string") {
      return res.status(400).json({ message: "Invalid userId" });
    }

    const user = await getUserByIdService(userId);

    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    return res.status(200).json(user);
  } catch (error) {
    return res.status(500).json({
      message: "Error fetching user",
      error,
    });
  }
};

export const updateUserController = async (
  req: Request,
  res: Response
) => {
  try {
    const { userId } = req.params;

    if (!userId || typeof userId !== "string") {
      return res.status(400).json({ message: "Invalid userId" });
    }

    const user = await updateUserService(userId, req.body);

    return res.status(200).json(user);
  } catch (error) {
    return res.status(500).json({
      message: "Error updating user",
      error,
    });
  }
};

export const deleteUserController = async (
  req: Request,
  res: Response
) => {
  try {
    const { userId } = req.params;

    if (!userId || typeof userId !== "string") {
      return res.status(400).json({ message: "Invalid userId" });
    }

    await deleteUserService(userId);

    return res.status(204).send();
  } catch (error) {
    return res.status(500).json({
      message: "Error deleting user",
      error,
    });
  }
};
