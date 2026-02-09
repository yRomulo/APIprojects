export interface LoginDto {
    email: string;
    password: string;
}

export interface TokenPayload {
    userId: string;
}