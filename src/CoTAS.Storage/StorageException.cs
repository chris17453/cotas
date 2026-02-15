namespace CoTAS.Storage;

public class StorageException : Exception
{
    public StorageException(string message) : base(message) { }
    public StorageException(string message, Exception inner) : base(message, inner) { }
}
