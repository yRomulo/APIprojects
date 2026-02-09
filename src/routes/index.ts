import { Router } from "express";
import authRoutes from "../modules/auth/auth.routes.js";
import userRouter from "../modules/users/user.routes.js";
import projectsRouter from "../modules/projects/project.routes.js";

const routes = Router();

routes.use("/auth", authRoutes);
routes.use("/users", userRouter);
routes.use("/projects", projectsRouter);

export default routes;