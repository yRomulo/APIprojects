import "dotenv/config";

export const env = {
  PORT: Number(process.env.PORT ?? 3333),
  DATABASE_URL: (() => {
    if (!process.env.DATABASE_URL) {
      throw new Error("DATABASE_URL is not defined");
    }
    return process.env.DATABASE_URL;
  })(),
};
