import { Router } from "express";
import {
  createProjectController,
  listProjectsController,
  listAllProjectsController,
  getProjectByIdController,
  updateProjectController,
  deleteProjectController,
} from "./project.controller.js";
import { authMiddleware } from "../../middlewares/auth.middleware.js";

const projectsRouter = Router();

/**
 * @swagger
 * tags:
 *   name: Projects
 *   description: Gerenciamento de projetos
 */

/**
 * @swagger
 * /projects:
 *   get:
 *     summary: Listar todos os projetos
 *     tags: [Projects]
 *     responses:
 *       200:
 *         description: Lista de todos os projetos
 *         content:
 *           application/json:
 *             schema:
 *               type: array
 *               items:
 *                 $ref: '#/components/schemas/Project'
 *   post:
 *     summary: Criar projeto (requer autenticação)
 *     tags: [Projects]
 *     security:
 *       - bearerAuth: []
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/CreateProjectDTO'
 *     responses:
 *       201:
 *         description: Projeto criado
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Project'
 *       401:
 *         description: Não autenticado
 *       500:
 *         description: Erro interno
 */
projectsRouter.get("/", listAllProjectsController);
projectsRouter.post("/", authMiddleware, createProjectController);

/**
 * @swagger
 * /projects/user/me:
 *   get:
 *     summary: Listar meus projetos (requer autenticação)
 *     tags: [Projects]
 *     security:
 *       - bearerAuth: []
 *     responses:
 *       200:
 *         description: Lista de meus projetos
 *         content:
 *           application/json:
 *             schema:
 *               type: array
 *               items:
 *                 $ref: '#/components/schemas/Project'
 *       401:
 *         description: Não autenticado
 */
projectsRouter.get("/user/me", authMiddleware, listProjectsController);

/**
 * @swagger
 * /projects/{projectId}:
 *   get:
 *     summary: Buscar projeto por ID
 *     tags: [Projects]
 *     parameters:
 *       - in: path
 *         name: projectId
 *         required: true
 *         schema:
 *           type: string
 *     responses:
 *       200:
 *         description: Projeto encontrado
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Project'
 *       404:
 *         description: Projeto não encontrado
 */
projectsRouter.get("/:projectId", getProjectByIdController);

/**
 * @swagger
 * /projects/{projectId}:
 *   put:
 *     summary: Atualizar projeto (requer autenticação)
 *     tags: [Projects]
 *     security:
 *       - bearerAuth: []
 *     parameters:
 *       - in: path
 *         name: projectId
 *         required: true
 *         schema:
 *           type: string
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/UpdateProjectDTO'
 *     responses:
 *       200:
 *         description: Projeto atualizado
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Project'
 *       401:
 *         description: Não autenticado
 *       403:
 *         description: Sem permissão
 */
projectsRouter.put("/:projectId", authMiddleware, updateProjectController);

/**
 * @swagger
 * /projects/{projectId}:
 *   delete:
 *     summary: Deletar projeto (requer autenticação)
 *     tags: [Projects]
 *     security:
 *       - bearerAuth: []
 *     parameters:
 *       - in: path
 *         name: projectId
 *         required: true
 *         schema:
 *           type: string
 *     responses:
 *       204:
 *         description: Projeto removido
 *       401:
 *         description: Não autenticado
 *       403:
 *         description: Sem permissão
 */
projectsRouter.delete("/:projectId", authMiddleware, deleteProjectController);

export default projectsRouter;