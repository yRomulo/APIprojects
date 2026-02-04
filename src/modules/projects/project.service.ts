import prisma from "../../config/prisma.js";
import type { CreateProjectDTO } from "./project.types.js";

export async function createProjectService(data: CreateProjectDTO) {
  return prisma.project.create({
    data: {
      title: data.title,
      userId: data.userId,
    },
  });
}

export async function listProjectsService(userId: string) {
  return prisma.project.findMany({
    where: { userId },
  });
}

export async function listAllProjectsService() {
  return prisma.project.findMany();
}

export async function getProjectByIdService(projectId: string) {
  return prisma.project.findUnique({
    where: { id: projectId },
  });
}

export async function updateProjectService(
  projectId: string,
  data: Partial<CreateProjectDTO>
) {
  return prisma.project.update({
    where: { id: projectId },
    data,
  });
}

export async function deleteProjectService(projectId: string) {
  return prisma.project.delete({
    where: { id: projectId },
  });
}
