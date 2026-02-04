try {
  console.log("Starting server...");
  const { env } = await import("./config/env.js");
  console.log("Env imported:", env);
  const { default: app } = await import("./app.js");
  console.log("App imported");

  app.listen(env.PORT, () => {
    console.log(`Server is running on port ${env.PORT}`);
  });
} catch (error) {
  console.error("Error starting server:", error);
  process.exit(1);
}