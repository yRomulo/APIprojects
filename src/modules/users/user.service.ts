import prisma from "../../config/prisma.js";
import type { CreateUserDTO } from "./user.types.js";

export async function createUserService(data: CreateUserDTO) {
  return prisma.user.create({
    data: {
      name: data.name,
      email: data.email,
      password: data.password,
    },
  });
}

export async function findUserByEmail(email: string) {
  return prisma.user.findUnique({
    where: { email },
  });
}

export async function listUsersService() {
  return prisma.user.findMany({
    select: {
      id: true,
      name: true,
      email: true,
      createdAt: true,
    },
  });
}

export async function getUserByIdService(userId: string) {
  return prisma.user.findUnique({
    where: { id: userId },
    select: {
      id: true,
      name: true,
      email: true,
      createdAt: true,
    },
  });
}

export async function updateUserService(
  userId: string,
  data: Partial<CreateUserDTO>
) {
  return prisma.user.update({
    where: { id: userId },
    data,
    select: {
      id: true,
      name: true,
      email: true,
      createdAt: true,
    },
  });
}

export async function deleteUserService(userId: string) {
  return prisma.user.delete({
    where: { id: userId },
  });
}
