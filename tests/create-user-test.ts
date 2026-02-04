import prisma from "../src/config/prisma.js";

async function main() {
  const user = await prisma.user.create({
    data: {
      name: "Rômulo",
      email: "romulo@email.com",
      password: "123456",
    },
  });

  console.log(user);
}

main()
  .catch(console.error)
  .finally(() => prisma.$disconnect());
