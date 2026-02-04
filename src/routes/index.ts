import { Router } from "express";
import userRouter from "../modules/users/user.routes.js";
import projectRoutes from "../modules/projects/project.routes.js";

const routes = Router();

routes.use("/users", userRouter);
routes.use("/projects", projectRoutes);

export default routes;