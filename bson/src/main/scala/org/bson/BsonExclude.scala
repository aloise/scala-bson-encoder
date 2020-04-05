package org.bson

/** This Bson would not be included as the field in the BsonDocument, for example `None` */
case object BsonExclude extends BsonValue {
  override def getBsonType: BsonType = throw new IllegalAccessError("This BsonType should not be stored")
}
