FROM mcr.microsoft.com/dotnet/sdk:5.0.100-alpine3.12-amd64

WORKDIR /app
COPY . /app

RUN cd backend && dotnet publish -c Release -r linux-x64 --self-contained false

RUN apk add yarn
RUN cd web && yarn && yarn webpack --mode production

FROM mcr.microsoft.com/dotnet/runtime:5.0.0-alpine3.12-amd64

WORKDIR /app
COPY --from=0 /app/backend/bin/Release/net5.0/linux-x64/publish .
COPY --from=0 /app/web/public ./public

EXPOSE 8090

ENTRYPOINT ["dotnet", "backend.dll"]
