Chatmap API
===========

Authentication
--------------

Set header `Cookie` to `password=sadmcasldkfjsdclasdkcmascdmklasdm` for read access.
For write access contact `HeNine`.

HTTPS connection is required.

REST API
--------

###Endpoint: `/api/chatizen`
    
* Method: `GET`
* Access: read
* Return: 
    * Status: 200
    * Content type: application/json

Returns all chatizens as a JSON list.

###Endpoint: `/api/chatizen/<name>`

* Method: `GET`
* Access: read
* Return:
    * Status: 200
    * Content type: application/json

Returns chatizen with name `<name>`. The name must match `^[a-z0-9]+$`.

* Method: `POST`
* Access: write
* Return:
    * Status: 204

Sets coordinates for chatizen with name `<name>`. 
Content of the message must be of the form `{"lat" : number, "lon" : number}`.

* Method: `DELETE`
* Access: write
* Return:
    * Status: 204
    
Delete chatizen with name `<name>`.

