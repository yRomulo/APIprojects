import { Router } from "express";
import {
  createProjectController,
  listProjectsController,
  listAllProjectsController,
  getProjectByIdController,
  updateProjectController,
  deleteProjectController,
} from "./project.controller.js";

const router = Router();

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
 *     summary: Criar projeto
 *     tags: [Projects]
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
 *       500:
 *         description: Erro interno
 */
router.get("/", listAllProjectsController);
router.post("/", createProjectController);

/**
 * @swagger
 * /projects/user/{userId}:
 *   get:
 *     summary: Listar projetos de um usuário
 *     tags: [Projects]
 *     parameters:
 *       - in: path
 *         name: userId
 *         required: true
 *         schema:
 *           type: string
 *     responses:
 *       200:
 *         description: Lista de projetos
 *         content:
 *           application/json:
 *             schema:
 *               type: array
 *               items:
 *                 $ref: '#/components/schemas/Project'
 */
router.get("/user/:userId", listProjectsController);

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
router.get("/:projectId", getProjectByIdController);

/**
 * @swagger
 * /projects/{projectId}:
 *   put:
 *     summary: Atualizar projeto
 *     tags: [Projects]
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
 */
router.put("/:projectId", updateProjectController);

/**
 * @swagger
 * /projects/{projectId}:
 *   delete:
 *     summary: Deletar projeto
 *     tags: [Projects]
 *     parameters:
 *       - in: path
 *         name: projectId
 *         required: true
 *         schema:
 *           type: string
 *     responses:
 *       204:
 *         description: Projeto removido
 */
router.delete("/:projectId", deleteProjectController);

export default router;
