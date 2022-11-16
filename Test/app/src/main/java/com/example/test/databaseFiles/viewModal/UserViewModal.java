package com.example.test.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.test.databaseFiles.entity.User;
import com.example.test.databaseFiles.repository.UserRepository;

import java.util.List;

public class UserViewModal extends AndroidViewModel {

    private UserRepository userRepository;
    private List<User> allUsers;

    public UserViewModal(@NonNull Application application) {
        super(application);
        userRepository = new UserRepository(application);
        allUsers = userRepository.getAllUsers();
    }

    public void insert(User user) {
        userRepository.insert(user);
    }

    public void update(User user) {
        userRepository.update(user);
    }

    public void delete(User user) {
        userRepository.delete(user);
    }

    public User findUser(int userID){
        return userRepository.findUser(userID);
    }

    public List<User> getAllUsers() {
        return allUsers;
    }
}
