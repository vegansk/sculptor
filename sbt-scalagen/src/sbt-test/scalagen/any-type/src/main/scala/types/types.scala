import kantan.xpath._
import kantan.xpath.implicits._
import cats.implicits._

package object types {

  final case class MeetingNotificationExtension(
    value: Either[nsd.XtnsnMeetingNotification, fes.MeetingNotificationIdentificationInfo]
  ) extends AnyVal

  object MeetingNotificationExtension {
    implicit val MeetingNotificationExtensionNodeDecoder: NodeDecoder[MeetingNotificationExtension] =
      NodeDecoder[nsd.XtnsnMeetingNotification]
        .map(v => MeetingNotificationExtension(v.asLeft[fes.MeetingNotificationIdentificationInfo])) orElse
        NodeDecoder.fromFound[fes.MeetingNotificationIdentificationInfo](
          Query[fes.MeetingNotificationIdentificationInfo](xp"./MeetingNotificationIdentificationInfo").eval(_)
        ).map(v => MeetingNotificationExtension(v.asRight[nsd.XtnsnMeetingNotification])) orElse
        NodeDecoder.fromFound(_ => DecodeError.TypeError("Can't parse extension").asLeft)
  }

}