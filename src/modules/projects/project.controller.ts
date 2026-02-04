import type { Request, Response } from "express";
import {
    createProjectService,
    listProjectsService,
    listAllProjectsService,
    getProjectByIdService,
    updateProjectService,
    deleteProjectService,
} from "./project.service.js";
import type { CreateProjectDTO } from "./project.types.js";

export const createProjectController = async (
    req: Request<unknown, unknown, CreateProjectDTO>,
    res: Response
) => {
    try {
        const project = await createProjectService(req.body);
        return res.status(201).json(project);
    } catch (error) {
        return res.status(500).json({
            message: "Error creating project",
            error,
        });
    }
};

export const listProjectsController = async (
    req: Request<{ userId: string }>,
    res: Response
) => {
    try {
        const { userId } = req.params;

        if (!userId || typeof userId !== "string") {
            return res.status(400).json({
                message: "Invalid userId parameter",
            });
        }

        const projects = await listProjectsService(userId);
        return res.status(200).json(projects);
    } catch (error) {
        return res.status(500).json({
            message: "Error listing projects",
            error,
        });
    }
};

export const listAllProjectsController = async (
    req: Request,
    res: Response
) => {
    try {
        const projects = await listAllProjectsService();
        return res.status(200).json(projects);
    } catch (error) {
        return res.status(500).json({
            message: "Error listing all projects"
        });
    }   
};

export const getProjectByIdController = async (
    req: Request<{ projectId: string }>,
    res: Response
) => {
    try {
        const { projectId } = req.params;

        if (!projectId || typeof projectId !== "string") {
            return res.status(400).json({ message: "Invalid projectId" });
        }

        const project = await getProjectByIdService(projectId);

        if (!project) {
            return res.status(404).json({ message: "Project not found" });
        }

        return res.status(200).json(project);
    } catch (error) {
        return res.status(500).json({
            message: "Error fetching project",
            error,
        });
    }
};

export const updateProjectController = async (
    req: Request<{ projectId: string }, unknown, Partial<CreateProjectDTO>>,
    res: Response
) => {
    try {
        const { projectId } = req.params;

        if (!projectId || typeof projectId !== "string") {
            return res.status(400).json({ message: "Invalid projectId" });
        }

        const project = await updateProjectService(projectId, req.body);

        if (!project) {
            return res.status(404).json({ message: "Project not found" });
        }

        return res.status(200).json(project);
    } catch (error) {
        return res.status(500).json({
            message: "Error updating project",
            error,
        });
    }
};

export const deleteProjectController = async (
    req: Request<{ projectId: string }>,
    res: Response
) => {
    try {
        const { projectId } = req.params;

        if (!projectId || typeof projectId !== "string") {
            return res.status(400).json({ message: "Invalid projectId" });
        }

        const project = await deleteProjectService(projectId);

        if (!project) {
            return res.status(404).json({ message: "Project not found" });
        }

        return res.status(204).send();
    } catch (error) {
        return res.status(500).json({
            message: "Error deleting project",
            error,
        });
    }
};
