package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.{Role, RoleCode, User, UserId, UserToRole}
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import zio.ZLayer
import zio.macros.accessible
import module4.phoneBook.db

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
        userRepo.list()


        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = {
            userRepo.list().flatMap{ users =>
              ZIO.foreach(users) {user =>
                  userRepo.userRoles(UserId(user.id))
                    .map(roles => UserDTO(user, roles.toSet))
              }
            }
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] =
            transaction{
                for {
                    newUser <- userRepo.createUser(user)
                    _ <- userRepo.insertRoleToUser(roleCode, UserId(user.id))
                    roles <- userRepo.userRoles(UserId(newUser.id))
                } yield UserDTO(user, roles.toSet)
            }
        
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = {
            userRepo.listUsersWithRole(roleCode).flatMap{ users =>
              ZIO.foreach(users){user =>
                userRepo.userRoles(UserId(user.id))
                  .map(roles => UserDTO(user, roles.toSet))
              }
            }
        }
        
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromService(userRepo => new Impl(userRepo))
}

case class UserDTO(user: User, roles: Set[Role])