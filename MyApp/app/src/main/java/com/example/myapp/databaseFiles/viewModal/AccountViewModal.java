package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.repository.UserRepository;

import java.util.List;

public class AccountViewModal extends AndroidViewModel {

    private UserRepository userRepository;
    private User user;

    public AccountViewModal(@NonNull Application application) {
        super(application);
        userRepository = new UserRepository(application);
        user = null;
    }

    public void createUser(Integer userID, String username, String password){
        user = new User(userID, username, password);
    }

    public void insert(User newUser) {
        long userID = userRepository.insert(newUser);
        user = new User((int) userID, newUser.getUsername(), newUser.getPassword());
        System.out.println(user.getUserID() + " " + user.getUsername() + " " + user.getPassword());
    }

    public void delete(){
        userRepository.delete(user);
        user = new User(0, null, null);
    }

    public void changeUsername(String newUsername){
        user.setUsername(newUsername);
        userRepository.update(user);
    }

    public void changePassword(String newPassword){
        user.setPassword(newPassword);
        userRepository.update(user);
    }

    public boolean validateUsername(String username){
        return findUser(username).size() == 0;
    }

    public List<User>findUser(String username){
        return userRepository.findUser(username);
    }

    public User getUser() {
        return user;
    }
}
